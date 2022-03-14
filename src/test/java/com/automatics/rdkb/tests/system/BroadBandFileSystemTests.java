/*
 * Copyright 2021 Comcast Cable Communications Management, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.automatics.rdkb.tests.system;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandFileSystemTests extends AutomaticsTestBase {

	/**
	 * Test to verify nvram file system layout validation
	 * <ol>
	 * <li>Check whether nvram is properly mounted and it has read- write permission
	 * after code download</li>
	 * <li>Verify the file access by creating a dummy file in nvram</li>
	 * </ol>
	 * 
	 * @param device instance of {@link Dut}
	 * 
	 * @author Karthick Pandiyan
	 * @refactor anandam
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-FS-LAYOUT-1001", testDecription = "nvram file system layout validation")
	public void nvramFileSystemLayout(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-FS-LAYOUT-001";
		String stepNum = null;
		String errorMessage = null;
		boolean status = false;
		String response = null;
		BroadBandResultObject broadBandResultObject = null;
		// Variable Declaration Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-FS-LAYOUT-1001");
		LOGGER.info("TEST DESCRIPTION: nvram file system layout validation");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Check whether nvram is properly mounted and it has read- write permission after code download");
		LOGGER.info("2. Verify the file access by creating a dummy file in nvram");

		LOGGER.info("#######################################################################################");

		try {

			LOGGER.info("**********************************************************************************");

			stepNum = "s1";
			errorMessage = "nvram is not properly mounted after code download";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 1: DESCRIPTION : Check whether nvram is properly mounted and it has read- write permission after code download");
			LOGGER.info(
					"STEP 1: ACTION : \"check mounted status using mount command(mount | grep -w \"nvram\") and verify read-write permission in nvram\"");
			LOGGER.info(
					"STEP 1: EXPECTED : nvram should be properly mounted and should have read-write permission(mtd:data on /nvram type jffs2 (rw,relatime)");
			LOGGER.info("**********************************************************************************");

			response = tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
					BroadBandCommandConstants.MOUNT_COMMAND, BroadBandTestConstants.NVRAM_PARTITION));

			broadBandResultObject = BroadBandCommonUtils.verifyMountStatusAndReadWritePermission(device, tapEnv,
					response, BroadBandTestConstants.STRING_PARTITION_NVRAM,
					BroadBandTestConstants.PATTERN_GET_READ_WRITE_PERMISSION_NVRAM_PARTITION,
					BroadBandTestConstants.STRING_READ_WRITE_PARTITION);

			status = broadBandResultObject.isStatus();

			if (status) {
				LOGGER.info(
						"STEP 1: ACTUAL : Build is having a persistent 'nvram' partition for user settings and configuration");
			} else {
				errorMessage = broadBandResultObject.getErrorMessage();
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s2";
			errorMessage = "Failed to create file with read write access in nvram";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Verify the file access by creating a dummy file in nvram");
			LOGGER.info(
					"STEP 2: ACTION : Create a dummy file in nvram(touch /nvram/dummytest.sh) and check for access(ls -la /nvram/dummytest.sh)");
			LOGGER.info(
					"STEP 2: EXPECTED : The file created in nvram should have read write access(-rw-r--r--  1 root  root  0 Jun 14 04:21 /nvram/dummytest.sh)");
			LOGGER.info("**********************************************************************************");

			status = BroadBandCommonUtils.createAndVerifyOwnerReadWritePermission(tapEnv, device,
					BroadBandTestConstants.NVRAM_DUMMY_TEST);

			if (status) {
				LOGGER.info("STEP 2: ACTUAL : Created file with read write access in nvram");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {

			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			LOGGER.info("POST-CONDITION : DESCRIPTION : Remove dummy file");
			LOGGER.info("POST-CONDITION : ACTION : Remove the dummy file permanently (rm -rf  /nvram/dummytest.sh)");
			LOGGER.info("POST-CONDITION : EXPECTED : Created dummy file should be removed");

			status = BroadBandCommonUtils.removeFileAndVerifyStatus(tapEnv, device,
					BroadBandTestConstants.NVRAM_DUMMY_TEST);

			if (status) {
				LOGGER.info("POST-CONDITION : ACTUAL : Post condition executed successfully");
			} else {
				LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed");
			}
			LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-FS-LAYOUT-1001");
	}

	/**
	 * Test to verify nvram2 file system layout validation
	 * <ol>
	 * <li>Check whether nvram2 is properly mounted and it has read- write
	 * permission after code download</li>
	 * <li>Verify the file access by creating a dummy file in nvram2</li>
	 * </ol>
	 * 
	 * @param device instance of {@link Dut}
	 * 
	 * @author Karthick Pandiyan
	 * @Refactor Alan_Bivera
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-FS-LAYOUT-1002")
	public void nvram2FileSystemLayout(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-FS-LAYOUT-002";
		String stepNum = null;
		String errorMessage = null;
		boolean status = false;
		String response = null;
		BroadBandResultObject broadBandResultObject = null;
		// Variable Declaration Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-FS-LAYOUT-1002");
		LOGGER.info("TEST DESCRIPTION: nvram2 file system layout validation");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info(
				"1. Check whether nvram2 is properly mounted and it has read- write permission after code download");
		LOGGER.info("2. Verify the file access by creating a dummy file in nvram2");

		LOGGER.info("#######################################################################################");

		try {

			stepNum = "s1";
			errorMessage = "nvram2 is not properly mounted after code download";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 1: DESCRIPTION : Check whether nvram2 is properly mounted and it has read- write permission after code download");
			LOGGER.info(
					"STEP 1: ACTION : \"check mounted status using mount command(mount | grep -w \"nvram2\") and verify read-write permission in nvram2\"");
			LOGGER.info(
					"STEP 1: EXPECTED : nvram2 should be properly mounted and should have read-write permission(mtd:data on /nvram2 type jffs2 (rw,relatime))");
			LOGGER.info("**********************************************************************************");

			response = tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
					BroadBandCommandConstants.MOUNT_COMMAND, BroadBandTestConstants.NVRAM2_PARTITION));

			broadBandResultObject = BroadBandCommonUtils.verifyMountStatusAndReadWritePermission(device, tapEnv,
					response, BroadBandTestConstants.STRING_PARTITION_NVRAM2,
					BroadBandTestConstants.PATTERN_GET_READ_WRITE_PERMISSION_NVRAM2_PARTITION,
					BroadBandTestConstants.STRING_READ_WRITE_PARTITION);

			status = broadBandResultObject.isStatus();

			if (status) {
				LOGGER.info("STEP 1: ACTUAL : Build is having a persistent 'nvram2' partition for logs");
			} else {
				errorMessage = broadBandResultObject.getErrorMessage();
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s2";
			errorMessage = "Failed to create file with read write access in nvram2";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Verify the file access by creating a dummy file in nvram2");
			LOGGER.info(
					"STEP 2: ACTION : Create a dummy file in nvram2(touch /nvram2/dummytest.sh) and check for access(ls -la /nvram2/dummytest.sh)");
			LOGGER.info(
					"STEP 2: EXPECTED : The file created in nvram2 should have read write access(-rw-r--r--   1 root   root   0 Jun 14 15:10 /nvram2/dummytest.sh)");
			LOGGER.info("**********************************************************************************");

			status = BroadBandCommonUtils.createAndVerifyOwnerReadWritePermission(tapEnv, device,
					BroadBandTestConstants.NVRAM2_DUMMY_TEST);

			if (status) {
				LOGGER.info("STEP 2: ACTUAL : Created file with read write access in nvram2");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {

			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			LOGGER.info("POST-CONDITION : DESCRIPTION : Remove dummy file");
			LOGGER.info("POST-CONDITION : ACTION : Remove the dummy file permanently (rm -rf  /nvram2/dummytest.sh)");
			LOGGER.info("POST-CONDITION : EXPECTED : Created dummy file should be removed");

			status = BroadBandCommonUtils.removeFileAndVerifyStatus(tapEnv, device,
					BroadBandTestConstants.NVRAM2_DUMMY_TEST);

			if (status) {
				LOGGER.info("POST-CONDITION : ACTUAL : Post condition executed successfully");
			} else {
				LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed");
			}
			LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-FS-LAYOUT-1002");
	}

	/**
	 * Test to verify tmp file system layout validation
	 * <ol>
	 * <li>Check whether tmp is properly mounted and it has read- write permission
	 * after code download</li>
	 * <li>Check whether tmp is having temporary mount points</li>
	 * <li>Verify the file access by creating a dummy file in tmp</li>
	 * </ol>
	 * 
	 * @param device instance of {@link Dut}
	 * 
	 * @author Karthick Pandiyan
	 * @Refactor Alan_Bivera
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-FS-LAYOUT-1003")
	public void tmpFileSystemLayout(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-FS-LAYOUT-003";
		String stepNum = null;
		String errorMessage = null;
		boolean status = false;
		String response = null;
		BroadBandResultObject broadBandResultObject = null;
		// Variable Declaration Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-FS-LAYOUT-1003");
		LOGGER.info("TEST DESCRIPTION: tmp file system layout validation");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Check whether tmp is properly mounted and it has read- write permission after code download");
		LOGGER.info("2. Check whether tmp is having temporary mount points");
		LOGGER.info("3. Verify the file access by creating a dummy file in tmp");

		LOGGER.info("#######################################################################################");

		try {

			stepNum = "s1";
			errorMessage = "tmp is not properly mounted after code download";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 1: DESCRIPTION : Check whether tmp is properly mounted and it has read- write permission after code download");
			LOGGER.info(
					"STEP 1: ACTION : \"check mounted status using mount command(mount | grep -w \"tmp\") and verify read-write permission in tmp\"");
			LOGGER.info(
					"STEP 1: EXPECTED : tmp should be properly mounted and should have read-write permission(tmpfs on /tmp type tmpfs (rw))");
			LOGGER.info("**********************************************************************************");

			response = tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
					BroadBandCommandConstants.MOUNT_COMMAND, BroadBandTestConstants.TMP_PARTITION));

			broadBandResultObject = BroadBandCommonUtils.verifyMountStatusAndReadWritePermission(device, tapEnv,
					response, BroadBandTestConstants.STRING_PARTITION_TMP,
					BroadBandTestConstants.PATTERN_GET_READ_WRITE_PERMISSION_TMP_PARTITION,
					BroadBandTestConstants.STRING_READ_WRITE_PARTITION);

			status = broadBandResultObject.isStatus();

			if (status) {
				LOGGER.info("STEP 1: ACTUAL : Build is mounted with read write permission in the tmp");
			} else {
				errorMessage = broadBandResultObject.getErrorMessage();
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s2";
			errorMessage = "tmp is not having temporary mount points";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Check whether tmp is having temporary mount points");
			LOGGER.info("STEP 2: ACTION : Verify the mount type using the command(mount | grep -w \"tmp\")");
			LOGGER.info("STEP 2: EXPECTED : tmp should have temporary mount point(tmpfs on /tmp type tmpfs (rw))");
			LOGGER.info("**********************************************************************************");

			status = CommonMethods.patternMatcher(response, BroadBandTestConstants.REGEX_TO_GET_TEMPORARY_MOUNT_POINT);
			if (status) {
				LOGGER.info("STEP 2: ACTUAL : tmp is having temporary mount point");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s3";
			errorMessage = "Failed to create file with read write access in tmp";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION : Verify the file access by creating a dummy file in tmp");
			LOGGER.info(
					"STEP 3: ACTION : Create a dummy file in tmp(touch /tmp/dummytest.sh) and check for access(ls -la  /tmp/dummytest.sh)");
			LOGGER.info(
					"STEP 3: EXPECTED : The file created in tmp should have read write access(-rw-r--r-- 1 root  root   0 Jun 14 15:14 /tmp/dummytest.sh)");
			LOGGER.info("**********************************************************************************");

			status = BroadBandCommonUtils.createAndVerifyOwnerReadWritePermission(tapEnv, device,
					BroadBandTestConstants.TMP_DUMMY_TEST);

			if (status) {
				LOGGER.info("STEP 3: ACTUAL : The file created in tmp is having read write access");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {

			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			LOGGER.info("POST-CONDITION : DESCRIPTION : Remove dummy file");
			LOGGER.info("POST-CONDITION : ACTION : Remove the dummy file permanently (rm -rf /tmp/dummytest.sh)");
			LOGGER.info("POST-CONDITION : EXPECTED : Created dummy file should be removed");

			status = BroadBandCommonUtils.removeFileAndVerifyStatus(tapEnv, device,
					BroadBandTestConstants.TMP_DUMMY_TEST);

			if (status) {
				LOGGER.info("POST-CONDITION : ACTUAL : Post condition executed successfully");
			} else {
				LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed");
			}
			LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-FS-LAYOUT-1003");
	}

	/**
	 * Test to verify minidumps file system layout validation
	 * <ol>
	 * <li>Check whether minidumps is properly mounted and it has read- write
	 * permission after code download</li>
	 * <li>Check whether minidumps is having temporary mount points</li>
	 * <li>Verify the file access by creating a dummy file in minidumps</li>
	 * </ol>
	 * 
	 * @param device instance of {@link Dut}
	 * 
	 * @author Karthick Pandiyan
	 * @Refactor Alan_Bivera
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-FS-LAYOUT-1004")
	public void minidumpFileSystemLayout(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-FS-LAYOUT-004";
		String stepNum = null;
		String errorMessage = null;
		boolean status = false;
		String response = null;
		BroadBandResultObject broadBandResultObject = null;
		// Variable Declaration Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-FS-LAYOUT-1004");
		LOGGER.info("TEST DESCRIPTION: minidumps file system layout validation");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info(
				"1. Check whether minidumps is properly mounted and it has read- write permission after code download");
		LOGGER.info("2. Check whether minidumps is having temporary mount points");
		LOGGER.info("3. Verify the file access by creating a dummy file in minidumps");

		LOGGER.info("#######################################################################################");

		try {

			stepNum = "s1";
			errorMessage = "minidumps is not properly mounted after code download";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 1: DESCRIPTION : Check whether minidumps is properly mounted and it has read- write permission after code download");
			LOGGER.info(
					"STEP 1: ACTION : \"check mounted status using mount command(mount | grep -w \"minidumps\") and verify read-write permission in minidumps\"");
			LOGGER.info(
					"STEP 1: EXPECTED : minidumps should be properly mounted and should have read-write permission(tmpfs on /minidumps type tmpfs (rw,nosuid,nodev,relatime,size=2048k,mode=755))");
			LOGGER.info("**********************************************************************************");

			response = tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
					BroadBandCommandConstants.MOUNT_COMMAND, BroadBandTestConstants.MINIDUMPS_PARTITION));

			broadBandResultObject = BroadBandCommonUtils.verifyMountStatusAndReadWritePermission(device, tapEnv,
					response, BroadBandTestConstants.STRING_PARTITION_MINIDUMPS,
					BroadBandTestConstants.PATTERN_GET_READ_WRITE_PERMISSION_MINIDUMPS_PARTITION,
					BroadBandTestConstants.STRING_READ_WRITE_PARTITION);

			status = broadBandResultObject.isStatus();

			if (status) {
				LOGGER.info("STEP 1: ACTUAL : minidumps is properly mounted and it has read-write permission");
			} else {
				errorMessage = broadBandResultObject.getErrorMessage();
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s2";
			errorMessage = "minidumps is not having temporary mount points";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Check whether minidumps is having temporary mount points");
			LOGGER.info("STEP 2: ACTION : Verify the mount type using the command(mount | grep -w \"minidumps\")");
			LOGGER.info(
					"STEP 2: EXPECTED : minidumps should have temporary mount point(tmpfs on /minidumps type tmpfs (rw,nosuid,nodev,relatime,size=2048k,mode=755))");
			LOGGER.info("**********************************************************************************");

			status = CommonMethods.patternMatcher(response, BroadBandTestConstants.REGEX_TO_GET_TEMPORARY_MOUNT_POINT);
			if (status) {
				LOGGER.info("STEP 2: ACTUAL : minidumps is having temporary mount points");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s3";
			errorMessage = "Failed to create file with read write access in minidumps";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION : Verify the file access by creating a dummy file in minidumps");
			LOGGER.info(
					"STEP 3: ACTION : Create a dummy file in minidumps(touch /minidumps/dummytest.txt) and check for access(ls -la /minidumps/dummytest.txt)");
			LOGGER.info(
					"STEP 3: EXPECTED : The file created in minidumps should have read write access(-rw-r--r-- 1 root  root  0 Jun 14 15:14 /tmp/dummytest.txt)");
			LOGGER.info("**********************************************************************************");

			boolean firstStatus = BroadBandCommonUtils.createAndVerifyOwnerReadWritePermission(tapEnv, device,
					BroadBandTestConstants.MINIDUMPS_DUMMY_TEST);
			// Just double checking the status again if periodic cleanup has deleted the
			// created file
			if (firstStatus) {
				status = firstStatus;
			} else {
				status = BroadBandCommonUtils.createAndVerifyOwnerReadWritePermission(tapEnv, device,
						BroadBandTestConstants.MINIDUMPS_DUMMY_TEST);
			}
			if (status) {
				LOGGER.info("STEP 3: ACTUAL : The file created in minidumps is having read write access");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {

			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			LOGGER.info("POST-CONDITION : DESCRIPTION : Remove dummy file");
			LOGGER.info(
					"POST-CONDITION : ACTION : Remove the dummy file permanently (rm -rf /minidumps/dummytest.txt)");
			LOGGER.info("POST-CONDITION : EXPECTED : Created dummy file should be removed");

			status = BroadBandCommonUtils.removeFileAndVerifyStatus(tapEnv, device,
					BroadBandTestConstants.MINIDUMPS_DUMMY_TEST);

			if (status) {
				LOGGER.info("POST-CONDITION : ACTUAL : Post condition executed successfully");
			} else {
				LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed");
			}
			LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-FS-LAYOUT-1004");
	}

	/**
	 * Test to verify rdklogs file system layout validation
	 * <ol>
	 * <li>Check whether rdklogs is properly mounted and it has read- write
	 * permission after code download</li>
	 * <li>Verify the file access by creating a dummy file in rdklogs</li>
	 * </ol>
	 * 
	 * @param device instance of {@link Dut}
	 * 
	 * @author Karthick Pandiyan
	 * @Refactor Alan_Bivera
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-FS-LAYOUT-1005")
	public void rdklogsFileSystemLayout(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-FS-LAYOUT-005";
		String stepNum = null;
		String errorMessage = null;
		boolean status = false;
		String response = null;
		BroadBandResultObject broadBandResultObject = null;
		// Variable Declaration Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-FS-LAYOUT-1005");
		LOGGER.info("TEST DESCRIPTION: rdklogs file system layout validation");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info(
				"1. Check whether rdklogs is properly mounted and it has read- write permission after code download");
		LOGGER.info("2. Verify the file access by creating a dummy file in rdklogs");

		LOGGER.info("#######################################################################################");

		try {

			stepNum = "s1";
			errorMessage = "rdklogs is not properly mounted after code download";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 1: DESCRIPTION : Check whether rdklogs is properly mounted and it has read- write permission after code download");
			LOGGER.info(
					"STEP 1: ACTION : \"check mounted status using mount command(mount | grep -w \"rdklogs\") and verify read-write permission in rdklogs\"");
			LOGGER.info(
					"STEP 1: EXPECTED : rdklogs should be properly mounted and should have read-write permission(tmpfs on /rdklogs type tmpfs (rw,nosuid,nodev,relatime,size=10240k,mode=755))");
			LOGGER.info("**********************************************************************************");

			response = tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
					BroadBandCommandConstants.MOUNT_COMMAND, BroadBandTestConstants.RDKLOGS_PARTITION));

			broadBandResultObject = BroadBandCommonUtils.verifyMountStatusAndReadWritePermission(device, tapEnv,
					response, BroadBandTestConstants.STRING_PARTITION_RDKLOGS,
					BroadBandTestConstants.PATTERN_GET_READ_WRITE_PERMISSION_RDKLOGS_PARTITION,
					BroadBandTestConstants.STRING_READ_WRITE_PARTITION);

			status = broadBandResultObject.isStatus();
			if (status) {
				LOGGER.info("STEP 1: ACTUAL : rdklog is properly mounted and has read-write permission");
			} else {
				errorMessage = broadBandResultObject.getErrorMessage();
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s2";
			errorMessage = "Failed to create file with read write access in rdklogs";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Verify the file access by creating a dummy file in rdklogs");
			LOGGER.info(
					"STEP 2: ACTION : Create a dummy file in tmp(touch /rdklogs/dummytest.sh) and check for access(ls -la  /rdklogs/dummytest.sh)");
			LOGGER.info(
					"STEP 2: EXPECTED : The file created in rdklogs should have read write access(-rw-r--r-- 1 root  root   0 Jun 14 15:16 /rdklogs/dummytest.sh)");
			LOGGER.info("**********************************************************************************");

			status = BroadBandCommonUtils.createAndVerifyOwnerReadWritePermission(tapEnv, device,
					BroadBandTestConstants.RDKLOGS_DUMMY_TEST);

			if (status) {
				LOGGER.info("STEP 2: ACTUAL : The file created in rdklogs is having read write access");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {

			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			LOGGER.info("POST-CONDITION : DESCRIPTION : Remove dummy file");
			LOGGER.info("POST-CONDITION : ACTION : Remove the dummy file permanently (rm -rf /rdklogs/dummytest.sh)");
			LOGGER.info("POST-CONDITION : EXPECTED : Created dummy file should be removed");

			status = BroadBandCommonUtils.removeFileAndVerifyStatus(tapEnv, device,
					BroadBandTestConstants.RDKLOGS_DUMMY_TEST);

			if (status) {
				LOGGER.info("POST-CONDITION : ACTUAL : Post condition executed successfully");
			} else {
				LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed");
			}
			LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-FS-LAYOUT-1005");
	}

	/**
	 * Test to verify cron and etc file system layout validation
	 * <ol>
	 * <li>Check whether cron is properly mounted and it has read- write permission
	 * after code download</li>
	 * <li>Check whether cron is having temporary mount points</li>
	 * <li>Verify the file access by creating a dummy file in etc</li>
	 * </ol>
	 * 
	 * @param device instance of {@link Dut}
	 * 
	 * @author Karthick Pandiyan
	 * @Refactor Alan_Bivera
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-FS-LAYOUT-1006")
	public void cronFileSystemLayout(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-FS-LAYOUT-006";
		String stepNum = null;
		String errorMessage = null;
		boolean status = false;
		String response = null;
		BroadBandResultObject broadBandResultObject = null;
		// Variable Declaration Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-FS-LAYOUT-1006");
		LOGGER.info("TEST DESCRIPTION: cron and etc file system layout validation");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Check whether cron is properly mounted and it has read- write permission after code download");
		LOGGER.info("2. Check whether cron is having temporary mount points");
		LOGGER.info("3. Verify the file access by creating a dummy file in etc");

		LOGGER.info("#######################################################################################");

		try {

			stepNum = "s1";
			errorMessage = "cron is not properly mounted after code download";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 1: DESCRIPTION : Check whether cron is properly mounted and it has read- write permission after code download");
			LOGGER.info(
					"STEP 1: ACTION : \"check mounted status using mount command(mount | grep -w \"cron\") and verify read-write permission in cron\"");
			LOGGER.info(
					"STEP 1: EXPECTED : cron should be properly mounted and should have read-write permission(tmpfs on /etc/cron type tmpfs (rw,relatime))");
			LOGGER.info("**********************************************************************************");

			response = tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
					BroadBandCommandConstants.MOUNT_COMMAND, BroadBandTestConstants.CRON_PARTITION));

			broadBandResultObject = BroadBandCommonUtils.verifyMountStatusAndReadWritePermission(device, tapEnv,
					response, BroadBandTestConstants.STRING_PARTITION_CRON,
					BroadBandTestConstants.PATTERN_GET_READ_WRITE_PERMISSION_CRON_PARTITION,
					BroadBandTestConstants.STRING_READ_WRITE_PARTITION);

			status = broadBandResultObject.isStatus();

			if (status) {
				LOGGER.info("STEP 1: ACTUAL : cron is properly mounted and it has read-write permission");
			} else {
				errorMessage = broadBandResultObject.getErrorMessage();
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s2";
			errorMessage = "cron is not having temporary mount points";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Check whether cron is having temporary mount points");
			LOGGER.info("STEP 2: ACTION : Verify the mount type using the command(mount | grep -w \"cron\")");
			LOGGER.info(
					"STEP 2: EXPECTED : cron should have temporary mount point(tmpfs on /etc/cron type tmpfs (rw,relatime))");
			LOGGER.info("**********************************************************************************");

			status = CommonMethods.patternMatcher(response, BroadBandTestConstants.REGEX_TO_GET_TEMPORARY_MOUNT_POINT);

			if (status) {
				LOGGER.info("STEP 2: ACTUAL : cron is having temporary mount point");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s3";
			errorMessage = "Created file inside etc folder";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION : Verify the file access by creating a dummy file in etc");
			LOGGER.info("STEP 3: ACTION : Create a dummy file in tmp(touch /etc/dummytest.sh) and check for access");
			LOGGER.info(
					"STEP 3: EXPECTED : File should not be created in etc folder(touch: /etc/dummytest.sh: Read-only file system)");
			LOGGER.info("**********************************************************************************");

			status = !BroadBandCommonUtils.createAndVerifyOwnerReadWritePermission(tapEnv, device,
					BroadBandTestConstants.ETC_DUMMY_TEST);

			if (status) {
				LOGGER.info("STEP 3: ACTUAL : File is not created in etc folder");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-FS-LAYOUT-1006");
	}

	/**
	 * Test to verify dhcp_static_hosts file system layout validation
	 * <ol>
	 * <li>Check whether dhcp_static_hosts is properly mounted and it has read-
	 * write permission after code download</li>
	 * <li>Check whether dhcp_static_hosts is having temporary mount points</li>
	 * </ol>
	 * 
	 * @param device instance of {@link Dut}
	 * 
	 * @author Karthick Pandiyan
	 * @Refactor Alan_Bivera
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-FS-LAYOUT-1007")
	public void dhcpStaticHostsFileSystemLayout(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-FS-LAYOUT-007";
		String stepNum = null;
		String errorMessage = null;
		boolean status = false;
		String response = null;
		BroadBandResultObject broadBandResultObject = null;
		// Variable Declaration Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-FS-LAYOUT-1007");
		LOGGER.info("TEST DESCRIPTION: dhcp_static_hosts file system layout validation");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info(
				"1. Check whether dhcp_static_hosts is properly mounted and it has read- write permission after code download");
		LOGGER.info("2. Check whether dhcp_static_hosts is having temporary mount points");

		LOGGER.info("#######################################################################################");

		try {

			stepNum = "s1";
			errorMessage = "dhcp_static_hosts is not properly mounted after code download";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 1: DESCRIPTION : Check whether dhcp_static_hosts is properly mounted and it has read- write permission after code download");
			LOGGER.info(
					"STEP 1: ACTION : \"check mounted status using mount command(mount | grep -w \"dhcp_static_hosts\") and verify read-write permission in dhcp_static_hosts\"");
			LOGGER.info(
					"STEP 1: EXPECTED : dhcp_static_hosts should be properly mounted and should have read-write permission(tmpfs on /etc/dhcp_static_hosts type tmpfs (rw,relatime))");
			LOGGER.info("**********************************************************************************");

			response = tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
					BroadBandCommandConstants.MOUNT_COMMAND, BroadBandTestConstants.DHCP_STATIC_HOSTS_PARTITION));

			broadBandResultObject = BroadBandCommonUtils.verifyMountStatusAndReadWritePermission(device, tapEnv,
					response, BroadBandTestConstants.STRING_PARTITION_DHCP_STATIC_HOSTS,
					BroadBandTestConstants.PATTERN_GET_READ_WRITE_PERMISSION_DHCP_STATIC_HOSTS_PARTITION,
					BroadBandTestConstants.STRING_READ_WRITE_PARTITION);

			status = broadBandResultObject.isStatus();

			if (status) {
				LOGGER.info("STEP 1: ACTUAL : dhcp_static_hosts is properly mounted and it has read-write permission");
			} else {
				errorMessage = broadBandResultObject.getErrorMessage();
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s2";
			errorMessage = "dhcp_static_hosts is not having temporary mount points";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Check whether dhcp_static_hosts is having temporary mount points");
			LOGGER.info(
					"STEP 2: ACTION : Verify the mount type using the command(mount | grep -w \"dhcp_static_hosts\")");
			LOGGER.info(
					"STEP 2: EXPECTED : dhcp_static_hosts should have temporary mount point(tmpfs on /etc/dhcp_static_hosts type tmpfs (rw,relatime))");
			LOGGER.info("**********************************************************************************");

			status = CommonMethods.patternMatcher(response, BroadBandTestConstants.REGEX_TO_GET_TEMPORARY_MOUNT_POINT);

			if (status) {
				LOGGER.info("STEP 2: ACTUAL : dhcp_static_hosts is having temporary mount point");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-FS-LAYOUT-1007");
	}

	/**
	 * Test to verify xupnp file system layout validation
	 * <ol>
	 * <li>Check whether xupnp is properly mounted and it has read- write permission
	 * after code download</li>
	 * <li>Check whether xupnp is having temporary mount points</li>
	 * </ol>
	 * 
	 * @param device instance of {@link Dut}
	 * 
	 * @author Karthick Pandiyan
	 * @Refactor Alan_Bivera
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-FS-LAYOUT-1008")
	public void xupnpFileSystemLayout(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-FS-LAYOUT-008";
		String stepNum = null;
		String errorMessage = null;
		boolean status = false;
		String response = null;
		BroadBandResultObject broadBandResultObject = null;
		// Variable Declaration Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-FS-LAYOUT-1008");
		LOGGER.info("TEST DESCRIPTION: xupnp file system layout validation");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Check whether xupnp is properly mounted and it has read- write permission after code download");
		LOGGER.info("2. Check whether xupnp is having temporary mount points");

		LOGGER.info("#######################################################################################");

		try {

			stepNum = "s1";
			errorMessage = "xupnp is not properly mounted after code download";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 1: DESCRIPTION : Check whether xupnp is properly mounted and it has read- write permission after code download");
			LOGGER.info(
					"STEP 1: ACTION : \"check mounted status using mount command(mount | grep -w \"xupnp\") and verify read-write permission in xupnp\"");
			LOGGER.info(
					"STEP 1: EXPECTED : xupnp should be properly mounted and should have read-write permission(tmpfs on /etc/xupnp type tmpfs (rw,relatime))");
			LOGGER.info("**********************************************************************************");

			response = tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
					BroadBandCommandConstants.MOUNT_COMMAND, BroadBandTestConstants.XUPNP_PARTITION));

			broadBandResultObject = BroadBandCommonUtils.verifyMountStatusAndReadWritePermission(device, tapEnv,
					response, BroadBandTestConstants.STRING_PARTITION_XUPNP,
					BroadBandTestConstants.PATTERN_GET_READ_WRITE_PERMISSION_XUPNP_PARTITION,
					BroadBandTestConstants.STRING_READ_WRITE_PARTITION);

			status = broadBandResultObject.isStatus();

			if (status) {
				LOGGER.info("STEP 1: ACTUAL : xupnp is properly mounted and it has read-write permission");
			} else {
				errorMessage = broadBandResultObject.getErrorMessage();
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s2";
			errorMessage = "xupnp is not having temporary mount points";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Check whether xupnp is having temporary mount points");
			LOGGER.info("STEP 2: ACTION : Verify the mount type using the command(mount | grep -w \"xupnp\")");
			LOGGER.info(
					"STEP 2: EXPECTED : xupnp should have temporary mount point(tmpfs on /etc/xupnp type tmpfs (rw,relatime))");
			LOGGER.info("**********************************************************************************");

			status = CommonMethods.patternMatcher(response, BroadBandTestConstants.REGEX_TO_GET_TEMPORARY_MOUNT_POINT);

			if (status) {
				LOGGER.info("STEP 2: ACTUAL : xupnp is having temporary mount points");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-FS-LAYOUT-1008");
	}

	/**
	 * Test to verify dibbler file system layout validation
	 * <ol>
	 * <li>Check whether dibbler is properly mounted and it has read- write
	 * permission after code download</li>
	 * <li>Check whether dibbler is having temporary mount points</li>
	 * </ol>
	 * 
	 * @param device instance of {@link Dut}
	 * 
	 * @author Karthick Pandiyan
	 * @Refactor Alan_Bivera
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-FS-LAYOUT-1009")
	public void dibblerFileSystemLayout(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-FS-LAYOUT-009";
		String stepNum = null;
		String errorMessage = null;
		boolean status = false;
		String response = null;
		BroadBandResultObject broadBandResultObject = null;
		// Variable Declaration Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-FS-LAYOUT-1009");
		LOGGER.info("TEST DESCRIPTION: dibbler file system layout validation");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info(
				"1. Check whether dibbler is properly mounted and it has read- write permission after code download");
		LOGGER.info("2. Check whether dibbler is having temporary mount points");

		LOGGER.info("#######################################################################################");

		try {

			LOGGER.info("**********************************************************************************");

			stepNum = "s1";
			errorMessage = "dibbler is not properly mounted after code download";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 1: DESCRIPTION : Check whether dibbler is properly mounted and it has read- write permission after code download");
			LOGGER.info(
					"STEP 1: ACTION : \"check mounted status using mount command(mount | grep -w \"dibbler\") and verify read-write permission in dibbler\"");
			LOGGER.info(
					"STEP 1: EXPECTED : dibbler should be properly mounted and should have read-write permission(tmpfs on /etc/dibbler type tmpfs (rw))");
			LOGGER.info("**********************************************************************************");

			response = tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
					BroadBandCommandConstants.MOUNT_COMMAND, BroadBandTestConstants.DIBBLER_PARTITION));

			broadBandResultObject = BroadBandCommonUtils.verifyMountStatusAndReadWritePermission(device, tapEnv,
					response, BroadBandTestConstants.STRING_PARTITION_DIBBLER,
					BroadBandTestConstants.PATTERN_GET_READ_WRITE_PERMISSION_DIBBLER_PARTITION,
					BroadBandTestConstants.STRING_READ_WRITE_PARTITION);

			status = broadBandResultObject.isStatus();

			if (status) {
				LOGGER.info("STEP 1: ACTUAL : dibbler is properly mounted and it has read-write permission");
			} else {
				errorMessage = broadBandResultObject.getErrorMessage();
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s2";
			errorMessage = "dibbler is not having temporary mount points";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Check whether dibbler is having temporary mount points");
			LOGGER.info("STEP 2: ACTION : Verify the mount type using the command(mount | grep -w \"dibbler\")");
			LOGGER.info(
					"STEP 2: EXPECTED : dibbler should have temporary mount point(tmpfs on /etc/dibbler type tmpfs (rw))");
			LOGGER.info("**********************************************************************************");

			status = CommonMethods.patternMatcher(response, BroadBandTestConstants.REGEX_TO_GET_TEMPORARY_MOUNT_POINT);

			if (status) {
				LOGGER.info("STEP 2: ACTUAL : dibbler is having temporary mount points");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-FS-LAYOUT-1009");
	}

	/**
	 * Test to verify resolv.conf file system layout validation and DNS value
	 * <ol>
	 * <li>Check whether resolv.conf is properly mounted and it has read- write
	 * permission after code download</li>
	 * <li>Check whether resolv.conf is having temporary mount points</li>
	 * <li>Verify whether Ipv4/ipv6 value has been updated in resolv.conf</li>
	 * </ol>
	 * 
	 * @param device instance of {@link Dut}
	 * 
	 * @author Karthick Pandiyan
	 * @Refactor Sruthi Santhosh
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-FS-LAYOUT-1010")
	public void resolvConfFileSystemLayout(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-FS-LAYOUT-010";
		String stepNum = null;
		String errorMessage = null;
		boolean status = false;
		String response = null;
		BroadBandResultObject broadBandResultObject = null;
		// Variable Declaration Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-FS-LAYOUT-1010");
		LOGGER.info("TEST DESCRIPTION: resolv.conf file system layout validation and DNS value");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info(
				"1. Check whether resolv.conf is properly mounted and it has read- write permission after code download");
		LOGGER.info("2. Check whether resolv.conf is having temporary mount points");
		LOGGER.info("3. Verify whether Ipv4/ipv6 value has been updated in resolv.conf");

		LOGGER.info("#######################################################################################");

		try {

			stepNum = "s1";
			errorMessage = "resolv.conf is not properly mounted after code download";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 1: DESCRIPTION : Check whether resolv.conf is properly mounted and it has read- write permission after code download");
			LOGGER.info(
					"STEP 1: ACTION : \"check mounted status using mount command(mount | grep -w \"resolv.conf\") and verify read-write permission in resolv.conf\"");
			LOGGER.info(
					"STEP 1: EXPECTED : resolv.conf should be properly mounted and should have read-write permission(tmpfs on /etc/resolv.conf type tmpfs (rw,relatime))");
			LOGGER.info("**********************************************************************************");

			response = tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
					BroadBandCommandConstants.MOUNT_COMMAND, BroadBandTestConstants.RESOLV_CONF_PARTITION));

			broadBandResultObject = BroadBandCommonUtils.verifyMountStatusAndReadWritePermission(device, tapEnv,
					response, BroadBandTestConstants.STRING_PARTITION_RESOLV_CONF,
					BroadBandTestConstants.PATTERN_GET_READ_WRITE_PERMISSION_RESOLV_CONF_PARTITION,
					BroadBandTestConstants.STRING_READ_WRITE_PARTITION);

			status = broadBandResultObject.isStatus();
			if (status) {
				LOGGER.info("STEP 1: ACTUAL : resolv.conf is properly mounted and it has read-write permission");
			} else {
				errorMessage = broadBandResultObject.getErrorMessage();
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s2";
			errorMessage = "resolv.conf is not having temporary mount points";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Check whether resolv.conf is having temporary mount points");
			LOGGER.info("STEP 2: ACTION : Verify the mount type using the command(mount | grep -w \"resolv.conf\")");
			LOGGER.info(
					"STEP 2: EXPECTED : resolv.conf should have temporary mount point(tmpfs on /etc/resolv.conf type tmpfs (rw,relatime))");
			LOGGER.info("**********************************************************************************");

			status = CommonMethods.patternMatcher(response, BroadBandTestConstants.REGEX_TO_GET_TEMPORARY_MOUNT_POINT);

			if (status) {
				LOGGER.info("STEP 2: ACTUAL : resolv.conf is having temporary mount point");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s3";
			errorMessage = "Failed to get global DNS IPV4/IPV6 value";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION : Verify whether Ipv4/ipv6 value has been updated in resolv.conf");
			LOGGER.info("STEP 3: ACTION : Execute command:cat /etc/resolv.conf");
			LOGGER.info(
					"STEP 3: EXPECTED : Value of all the nameserver should be of ipv4/ipv6 address(Example: nameserver 75.75.75.75) ");
			LOGGER.info("**********************************************************************************");

			response = tapEnv.executeCommandUsingSsh(device,
					BroadBandTestConstants.CAT_COMMAND + BroadBandTestConstants.RESOLVE_DOT_CONF_FILE);

			status = BroadBandCommonUtils.compareAllTheMatchedPattern(response,
					BroadBandTestConstants.PATTERN_GET_IPV4_OR_IPV6_SERVER_ADDRESS,
					BroadBandTestConstants.PATTERN_GET_NAMESPACE_SERVER_ADDRESS);

			if (status) {
				LOGGER.info("STEP 3: ACTUAL : Value of primary DNS is of ipv4/ipv6 address");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-FS-LAYOUT-1010");
	}

}
