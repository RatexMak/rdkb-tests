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

import java.util.ArrayList;
import java.util.List;

import org.apache.http.HttpStatus;
import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandRfcFeatureControlUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandFileSystemTests extends AutomaticsTestBase {

    /**
     * Test to verify nvram file system layout validation
     * <ol>
     * <li>Check whether nvram is properly mounted and it has read- write permission after code download</li>
     * <li>Verify the file access by creating a dummy file in nvram</li>
     * </ol>
     * 
     * @param device
     *            instance of {@link Dut}
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
     * <li>Check whether nvram2 is properly mounted and it has read- write permission after code download</li>
     * <li>Verify the file access by creating a dummy file in nvram2</li>
     * </ol>
     * 
     * @param device
     *            instance of {@link Dut}
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
     * <li>Check whether tmp is properly mounted and it has read- write permission after code download</li>
     * <li>Check whether tmp is having temporary mount points</li>
     * <li>Verify the file access by creating a dummy file in tmp</li>
     * </ol>
     * 
     * @param device
     *            instance of {@link Dut}
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
     * <li>Check whether minidumps is properly mounted and it has read- write permission after code download</li>
     * <li>Check whether minidumps is having temporary mount points</li>
     * <li>Verify the file access by creating a dummy file in minidumps</li>
     * </ol>
     * 
     * @param device
     *            instance of {@link Dut}
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
     * <li>Check whether rdklogs is properly mounted and it has read- write permission after code download</li>
     * <li>Verify the file access by creating a dummy file in rdklogs</li>
     * </ol>
     * 
     * @param device
     *            instance of {@link Dut}
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
     * <li>Check whether cron is properly mounted and it has read- write permission after code download</li>
     * <li>Check whether cron is having temporary mount points</li>
     * <li>Verify the file access by creating a dummy file in etc</li>
     * </ol>
     * 
     * @param device
     *            instance of {@link Dut}
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
     * <li>Check whether dhcp_static_hosts is properly mounted and it has read- write permission after code
     * download</li>
     * <li>Check whether dhcp_static_hosts is having temporary mount points</li>
     * </ol>
     * 
     * @param device
     *            instance of {@link Dut}
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
     * <li>Check whether xupnp is properly mounted and it has read- write permission after code download</li>
     * <li>Check whether xupnp is having temporary mount points</li>
     * </ol>
     * 
     * @param device
     *            instance of {@link Dut}
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
     * <li>Check whether dibbler is properly mounted and it has read- write permission after code download</li>
     * <li>Check whether dibbler is having temporary mount points</li>
     * </ol>
     * 
     * @param device
     *            instance of {@link Dut}
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
     * <li>Check whether resolv.conf is properly mounted and it has read- write permission after code download</li>
     * <li>Check whether resolv.conf is having temporary mount points</li>
     * <li>Verify whether Ipv4/ipv6 value has been updated in resolv.conf</li>
     * </ol>
     * 
     * @param device
     *            instance of {@link Dut}
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
		    "STEP 3: EXPECTED : Value of all the nameserver should be of ipv4/ipv6 address ");
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

    /**
     * To verify CA Bundle
     * <ol>
     * <li>Verify By Default DLCaStore.Enable RFC feature is disabled</li>
     * <li>Enable DLCaStore.Enable feature via RFC</li>
     * <li>Verify DLCaStore is enabled in caupdate.log</li>
     * <li>Verify package caupdatebundle is downloaded successfuly and signature verification success from
     * rdm_status.log</li>
     * <li>Verify caupdatebundle packages are downloaded successfully</li>
     * <li>Verify CADL bundle is found message from caupdate.log</li>
     * <li>Verify all certs are extracted to tmp folder</li>
     * <li>Verify that there are no symbolic links</li>
     * <li>Check the connection through curl to make sure that it refer from the newly mounted bundle</li>
     * <li>Move ca-cert*.crt and verify the connection and connection should fail</li>
     * <li>Disable DLCaStore.Enable feature via RFC</li>
     * <li>Verify DLCaStore is disabled from caupdate.log</li>
     * <li>Verify symbolic link for COMODO_RSA_Certification_Authority</li>
     * <li>Verify certs are removed, mount copybind MUST NOT be invoked on reboot if DLCerts are disabled by RFC</li>
     * </ol>
     * 
     * @author Taher Veeramgoanwala
     * @Refactor Sruthi Santhosh
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-CERTS-1001")
    public void ValidateCaBundle(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-CERTS-101";
	String stepNum = null;
	String errorMessage = null;
	boolean status = false;
	BroadBandResultObject result = null;
	String payLoadData = null;
	String response = null;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-CERTS-1001");
	LOGGER.info("TEST DESCRIPTION: To verify Ca bundle");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify By Default DLCaStore.Enable RFC feature is disabled");
	LOGGER.info("2. Enable DLCaStore.Enable feature via RFC");
	LOGGER.info("3. Verify DLCaStore is enabled in caupdate.log");
	LOGGER.info(
		"4. Verify package caupdatebundle is downloaded successfuly and signature verification success from rdm_status.log");
	LOGGER.info("5. Verify caupdatebundle packages are downloaded successfully");
	LOGGER.info("6. Verify CADL bundle is found message from caupdate.log");
	LOGGER.info("7. Verify all certs are extracted to tmp folder");
	LOGGER.info("8. Verify that there are no symbolic link </usr/share/> for certs");
	LOGGER.info("9. Check the connection through curl to make sure that it refer from the newly mounted bundle");
	LOGGER.info("10. Move ca-cert*.crt and verify the connection and connection should fail");
	LOGGER.info("11. Disable DLCaStore.Enable feature via RFC");
	LOGGER.info("12. Verify DLCaStore is disabled from caupdate.log");
	LOGGER.info("13. Verify symbolic link for COMODO_RSA_Certification_Authority");
	LOGGER.info(
		"14. Verify certs are removed, mount copybind MUST NOT be invoked on reboot if DLCerts are disabled by RFC");
	LOGGER.info("#######################################################################################");

	try {

	    stepNum = "s1";
	    errorMessage = "Failed to Default DLCaStore.Enable RFC feature as disabled";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify By Default DLCaStore.Enable RFC feature is disabled");
	    LOGGER.info(
		    "STEP 1: ACTION : Execute Command:tr181 Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.DLCaStore.Enable");
	    LOGGER.info("STEP 1: EXPECTED : By Default DLCaStore.Enable RFC feature should be disabled");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DLCASTORE, BroadBandTestConstants.FALSE);

	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : By Default DLCaStore.Enable RFC feature is disabled");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s2";
	    errorMessage = "Failed to enable DLCaStore.Enable RFC";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Enable DLCaStore.Enable feature via RFC");
	    LOGGER.info(
		    "STEP 2: ACTION : Enable device with tr181.Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.DLCaStore.Enable via RFC");
	    LOGGER.info("STEP 2: EXPECTED : DLCaStore.Enable should be enabled successfully");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandRfcFeatureControlUtils.enableOrDisableFeatureByRFC(tapEnv, device,
		    BroadBandTestConstants.CONFIGURABLE_CABUNDLE, BroadBandTestConstants.BOOLEAN_VALUE_TRUE);

	    if (status) {
		status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DLCASTORE, BroadBandTestConstants.TRUE);
		if (status) {
		    tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_SET_CABUNDLE_URL);
		}
	    }

	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : DLCaStore.Enable enabled successfully");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s3";
	    errorMessage = "Failed to log enable dlcastore in caupdate.log";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Verify DLCaStore is enabled in caupdate.log");
	    LOGGER.info(
		    "STEP 3: ACTION : Execute Command:grep -I \"RFC DLCaStore is enabled\" /rdklogs/logs/caupdate.log");
	    LOGGER.info("STEP 3: EXPECTED : Verify DLCAStore should be enabled in caupdate.log");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device,
		    tapEnv, BroadBandTraceConstants.LOG_MESSAGE_CABUNDLE_ENABLED,
		    BroadBandCommandConstants.CMD_GET_CABUNDLE_ENABLE_LOG, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : Verify DLCAStore enabled in caupdate.log");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s4";
	    errorMessage = "Failed to verify package signature";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 4: DESCRIPTION : Verify package caupdatebundle is downloaded successfuly and signature verification success from rdm_status.log");
	    LOGGER.info(
		    "STEP 4: ACTION : Execute Command:grep -I \"RSA Signature Validation Success\" /rdklogs/logs/rdm_status.log");
	    LOGGER.info("STEP 4: EXPECTED : Signature verification should be successful");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device,
		    tapEnv, BroadBandTraceConstants.LOG_MESSAGE_CABUNDLE_RDMLOG,
		    BroadBandCommandConstants.CMD_GET_CABUNDLE_RDMSTATUS_LOG,
		    BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : Signature verification is successful");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
		tapEnv.executeCommandUsingSsh(device, "cat /rdklogs/logs/rdm_status.log");
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s5";
	    errorMessage = "Failed to download caupdatebundle packages";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION : Verify caupdatebundle packages are downloaded successfully");
	    LOGGER.info("STEP 5: ACTION : Execute Command:ls /tmp/caupdatebundle/etc");
	    LOGGER.info("STEP 5: EXPECTED : caupdatebundle Packages should be downloaded successfully");
	    LOGGER.info("**********************************************************************************");

	    status = (CommonMethods.isFileExists(device, tapEnv, BroadBandCommandConstants.CMD_GET_CABUNDLE_DOWNLOAD));

	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : caupdatebundle Packages downloaded successfully");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);

	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s6";
	    errorMessage = "Failed to cadl bundle found in caupdate.log";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Verify CADL bundle is found message from caupdate.log");
	    LOGGER.info("STEP 6: ACTION : Execute command:grep -I \"Found CADL Bundle\" \"/rdklogs/logs/caupdate.log");
	    LOGGER.info("STEP 6: EXPECTED : CADL bundle found message should be logged in caupdate.log");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device,
		    tapEnv, BroadBandTraceConstants.LOG_MESSAGE_CABUNDLE_FOUND,
		    BroadBandCommandConstants.CMD_GET_CABUNDLE_ENABLE_LOG, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : CADL bundle found message logged in caupdate.log");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s7";
	    errorMessage = "Failed to extract certs to temp folder";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Verify all certs are extracted to tmp folder");
	    LOGGER.info("STEP 7: ACTION : Execute command:ls /tmp/shesc/");
	    LOGGER.info("STEP 7: EXPECTED : Certs should be extracted to /tmp/shesc folder");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandCommonUtils.doesDirectoryExistInArmConsole(device, tapEnv,
		    BroadBandCommandConstants.CMD_GET_CABUNDLE_SHADOWFOLDER,
		    BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
		    BroadBandTestConstants.TRUE).isStatus();

	    if (status) {
		LOGGER.info("STEP 7: ACTUAL : Certs extracted to /tmp/shesc folder");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s8";
	    errorMessage = "Failed to remove symbolic links for certs ";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION : Verify that there are no symbolic link </usr/share/> for certs");
	    LOGGER.info(
		    "STEP 8: ACTION : Execute command:ls -la /etc/ssl/certs/COMODO_RSA_Certification_Authority.pem");
	    LOGGER.info("STEP 8: EXPECTED : symbolic links</usr/share/> should not be present for certs");
	    LOGGER.info("**********************************************************************************");

	    response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_TO_LIST_CERTS);

	    status = CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
		    BroadBandTestConstants.SSL_CERTS_FOLDER);

	    if (status) {
		LOGGER.info("STEP 8: ACTUAL : symbolic links</usr/share/> is not present for certs");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s9";
	    errorMessage = "Connection failed with ca certs";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 9: DESCRIPTION : Check the connection through curl to make sure that it refer from the newly mounted bundle");
	    LOGGER.info("STEP 9: ACTION : Execute command:curl -v --cert-status https://google.com/");
	    LOGGER.info("STEP 9: EXPECTED : Connection should be successful using the ca certs.");
	    LOGGER.info("**********************************************************************************");

	    response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_TO_PING_GOOGLE);

	    status = CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
		    BroadBandTestConstants.SPEED_TEST_SECURITY_CERTIFICATE);

	    if (status) {
		LOGGER.info("STEP 9: ACTUAL : Connection successful using the ca certs");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s10";
	    errorMessage = "connection failed to use ca certs";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 10: DESCRIPTION : Move ca-cert*.crt and verify the connection and connection should fail");
	    LOGGER.info(
		    "STEP 10: ACTION : Execute comman:cd /tmp/shesc/mv ca-certificates.crt ca-certificates.crt-origcurl -v --cert-status https://google.com/mv ca-certificates.crt-orig ca-certificates.crtcurl -v --cert-status https://google.com/");
	    LOGGER.info("STEP 10: EXPECTED : Connection should fail since certs is moved to differrent location.");
	    LOGGER.info("**********************************************************************************");

	    tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_GET_CABUNDLE_MOVECERTIFICATE_BACK);
	    response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_TO_PING_GOOGLE);
	    status = CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
		    BroadBandTraceConstants.LOG_MESSAGE_CABUNDLE_ERROR);
	    // move back certs again
	    tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_GET_CABUNDLE_MOVECERTIFICATE);

	    if (status) {
		LOGGER.info("STEP 10: ACTUAL : Connection fails since certs is moved to differrent location");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s11";
	    errorMessage = "Failed to disable DLCaStore.Enable RFC";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 11: DESCRIPTION : Disable DLCaStore.Enable feature via RFC");
	    LOGGER.info(
		    "STEP 11: ACTION : Disable device with tr181.Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.DLCaStore.Enable via RFC");
	    LOGGER.info("STEP 11: EXPECTED : DLCaStore.Enable should be disabled successfully");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandRfcFeatureControlUtils.enableOrDisableFeatureByRFC(tapEnv, device,
		    BroadBandTestConstants.CONFIGURABLE_CABUNDLE, BroadBandTestConstants.BOOLEAN_VALUE_FALSE);

	    if (status) {
		LOGGER.info("STEP 11: ACTUAL : DLCaStore.Enable disabled successfully");
	    } else {
		LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
		tapEnv.executeCommandUsingSsh(device, "cat /rdklogs/logs/rfcscript.log");
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s12";
	    errorMessage = "Failed to log disable dlcastore in caupdate.log";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 12: DESCRIPTION : Verify DLCaStore is disabled from caupdate.log");
	    LOGGER.info(
		    "STEP 12: ACTION : Execute Command:grep -I \"RFC DLCaStore is not enabled\" /rdklogs/logs/caupdate.log");
	    LOGGER.info("STEP 12: EXPECTED : Verify DLCAStore should be disabled from caupdate.log");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device,
		    tapEnv, BroadBandTraceConstants.LOG_MESSAGE_CABUNDLE_DISABLED,
		    BroadBandCommandConstants.CMD_GET_CABUNDLE_ENABLE_LOG, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

	    if (status) {
		LOGGER.info("STEP 12: ACTUAL : Verify DLCAStore disabled from caupdate.log");
	    } else {
		LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
		tapEnv.executeCommandUsingSsh(device, "cat /rdklogs/logs/caupdate.log");
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s13";
	    errorMessage = "Failed to use symbolic links";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 13: DESCRIPTION : symbolic link for COMODO_RSA_Certification_Authority");
	    LOGGER.info(
		    "STEP 13: ACTION : Execute command:ls -la /etc/ssl/certs/COMODO_RSA_Certification_Authority.pem");
	    LOGGER.info("STEP 13: EXPECTED : symbolic links</usr/share/> should be present for certs");
	    LOGGER.info("**********************************************************************************");

	    response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_TO_LIST_CERTS);

	    status = CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
		    BroadBandTestConstants.PATH_FOR_CA_CERTS);

	    if (status) {
		LOGGER.info("STEP 13: ACTUAL : symbolic links</usr/share/> present for certs");
	    } else {
		LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s14";
	    errorMessage = "Failed to remove certs from tmp/shesc folder";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 14: DESCRIPTION : verify certs are removed, mount copybind MUST NOT be invoked on reboot if DLCerts are disabled by RFC");
	    LOGGER.info("STEP 14: ACTION : Execute Command:ls /tmp/shesc/");
	    LOGGER.info("STEP 14: EXPECTED : tmp/shesc folder is empty");
	    LOGGER.info("**********************************************************************************");

	    response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_TO_LIST_TMP_CERTS);

	    status = CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
		    BroadBandTestConstants.NO_SUCH_FILE_OR_DIRECTORY);

	    if (status) {
		LOGGER.info("STEP 14: ACTUAL : tmp/shesc folder is empty");
	    } else {
		LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
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
	    LOGGER.info("POST-CONDITION : DESCRIPTION : Remove DLCASTORE feature configuration from proxy xconf");
	    LOGGER.info("POST-CONDITION : ACTION : 1) remove rfc settings file\n2) Reboot the device");
	    LOGGER.info(
		    "POST-CONDITION : EXPECTED : DLCASTORE feature config should be delete from proxy xconf successfully");

	    status = (HttpStatus.SC_OK == BroadBandRfcFeatureControlUtils.clearSettingsInProxyXconfDcmServerForRDKB(
		    device, tapEnv, false, BroadBandTestConstants.CONFIGURABLE_CABUNDLE));

	    if (status) {
		LOGGER.info("POST-CONDITION : ACTUAL : Post condition executed successfully");
	    } else {
		LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed");
	    }
	    LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-CERTS-1001");
    }

    /**
     * Verify removal of SoundKernel, mtp-tools, libsven
     * 
     * <li>1. Verify binary executable module dmesg using BusyBox</li>
     * <li>2. Verify binary executable module kill using BusyBox</li>
     * <li>3. Verify binary executable module more using BusyBox</li>
     * <li>4. Verify binary executable module mount using BusyBox</li>
     * <li>5. Verify binary executable module umount using BusyBox</li>
     * <li>6. Verify binary executable module swapon using BusyBox</li>
     * <li>7. Verify binary executable module swapoff using BusyBox</li>
     * <li>8. Verify binary executable module fsck using BusyBox</li>
     * <li>9. Verify binary executable module hwclock using BusyBox</li>
     * <li>10. Verify binary executable module chrt using BusyBox</li>
     * <li>11. Verify binary executable module eject using BusyBox</li>
     * <li>12. Verify binary executable module flock using BusyBox</li>
     * <li>13. Verify binary executable module hexdump using BusyBox</li>
     * <li>14. Verify binary executable module logger using BusyBox</li>
     * <li>15. Verify binary executable module mesg using BusyBox</li>
     * <li>16. Verify binary executable module renice using BusyBox</li>
     * <li>17. Verify binary executable module setsid using BusyBox</li>
     * <li>18. Verify binary executable module losetup using BusyBox</li>
     * <li>19. Verify binary executable module fsck.minix using BusyBox</li>
     * 
     * @author ArunKumar Jayachandran
     * @refactor Said Hisham
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    TestGroup.NEW_FEATURE, TestGroup.SYSTEM })
    @TestDetails(testUID = "TC-RDKB-RM-MODULE-1002")
    public void testToVerifyBinariesUsingBusyBox(Dut device) {
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-RM-MODULE-1002");
	LOGGER.info("TEST DESCRIPTION: Test to verify binary executables are using BusyBox");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify binary executable module dmesg using BusyBox");
	LOGGER.info("2. Verify binary executable module kill using BusyBox");
	LOGGER.info("3. Verify binary executable module more using BusyBox");
	LOGGER.info("4. Verify binary executable module mount using BusyBox");
	LOGGER.info("5. Verify binary executable module umount using BusyBox");
	LOGGER.info("6. Verify binary executable module swapon using BusyBox");
	LOGGER.info("7. Verify binary executable module swapoff using BusyBox");
	LOGGER.info("8. Verify binary executable module fsck using BusyBox");
	LOGGER.info("9. Verify binary executable module hwclock using BusyBox");
	LOGGER.info("10. Verify binary executable module chrt using BusyBox");
	LOGGER.info("11. Verify binary executable module eject using BusyBox");
	LOGGER.info("12. Verify binary executable module flock using BusyBox");
	LOGGER.info("13. Verify binary executable module hexdump using BusyBox");
	LOGGER.info("14. Verify binary executable module logger using BusyBox");
	LOGGER.info("15. Verify binary executable module mesg using BusyBox");
	LOGGER.info("16. Verify binary executable module renice using BusyBox");
	LOGGER.info("17. Verify binary executable module setsid using BusyBox");
	LOGGER.info("18. Verify binary executable module losetup using BusyBox");
	LOGGER.info("19. Verify binary executable module fsck.minix using BusyBox");
	LOGGER.info("#######################################################################################");

	// variable declaration begins
	// Status of test script verification
	boolean status = false;
	// Test case id
	String testCaseId = "TC-RDKB-RM-MODULE-102";
	// Test step number
	String stepNumber = "s1";
	// String to store error message
	String errorMessage = null;
	// Variable to store atom sync available
	boolean isAtomSyncAvailable = false;
	// String to store response
	String response = null;
	// String to store command
	String command = null;
	// Integer to store step count
	int stepCount = BroadBandTestConstants.CONSTANT_1;
	// variable declaration ends

	// verify whether the atom sync is available in given device
	isAtomSyncAvailable = CommonMethods.isAtomSyncAvailable(device, tapEnv);
	for (String modules : BroadBandCommandConstants.EXECUTABLE_BINARY_LIST) {
	    try {
		stepNumber = "s" + stepCount;
		status = false;
		LOGGER.info("******************************************************************************");
		LOGGER.info("STEP " + stepCount + ": DESCRIPTION: Verify binary executable module " + modules
			+ " using BusyBox");
		LOGGER.info("STEP " + stepCount + ": ACTION: 1.ls -l " + modules
			+ " 2.if AtomSync is available, Execute command on Atom side: ls -l " + modules);
		LOGGER.info("STEP " + stepCount + ": EXPECTED: " + modules
			+ " binary module should use the BusyBox or should get no such file or directory");
		LOGGER.info("******************************************************************************");
		errorMessage = "Binary module " + modules + " not using busybox";
		command = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_LS_L,
			modules);
		response = tapEnv.executeCommandUsingSsh(device, command);
		if (CommonMethods.isNotNull(response)) {
		    status = CommonUtils.isGivenStringAvailableInCommandOutput(response,
			    BroadBandTestConstants.NO_SUCH_FILE_OR_DIRECTORY)
			    || CommonUtils.isGivenStringAvailableInCommandOutput(response,
				    BroadBandCommandConstants.PROCESS_BUSY_BOX);
		    // Execute command on ATOM console, if ATOM sync is available
		    if (status && isAtomSyncAvailable) {
			status = false;
			errorMessage = "Binary module " + modules + " not using busybox in ATOM side";
			response = tapEnv.executeCommandOnAtom(device, command);
			if (CommonMethods.isNotNull(response)) {
			    status = CommonUtils.isGivenStringAvailableInCommandOutput(response,
				    BroadBandTestConstants.NO_SUCH_FILE_OR_DIRECTORY)
				    || CommonUtils.isGivenStringAvailableInCommandOutput(response,
					    BroadBandCommandConstants.PROCESS_BUSY_BOX)
				    || CommonUtils.isGivenStringAvailableInCommandOutput(response,
					    BroadBandCommandConstants.UTIL_LINUX);
			}
		    }
		}
		if (status) {
		    LOGGER.info("STEP " + stepCount + ": ACTUAL: Successfully verified " + modules + " using busybox");
		} else {
		    LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
		// ##################################################################################################//
		stepCount = stepCount + 1;

	    } catch (Exception exception) {
		errorMessage = exception.getMessage();
		LOGGER.error("Exception Occurred while Verifying binary executables are using BusyBox" + errorMessage);
		CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status,
			errorMessage, false);
	    }
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-RM-MODULE-1002");
	// ###############################################################//
    }
    
    /**
     * Verify removal of SoundKernel, mtp-tools, libsven
     *
     * <li>1. Verify soundkernel module removed status</li>
     * <li>2. Verify libsven_modules removed status</li>
     * <li>3. Verify mtp-tools removed status</li>
     * <li>4. Verify ntpd process running status in Arm side</li>
     * <li>5. Verify ntpd client running status in Atom side</li>
     * <li>6. Verify COSAXcalibur.XML file removed status</li>
     * <li>7. Verify Verify files listed in Acceptance criteria 1 of removed status in Atom side</li>
     * <li>8. Verify Verify files listed in Acceptance criteria 2 of removed status in Atom side</li>
     * <li>9. Verify Atom Console files status from Atom side</li>
     * <li>10. STEP 10: Verify 13 Arm console files removed status from Arm side on all platforms</li>
     *
     * @author ArunKumar Jayachandran
     * @author RamaTeja Meduri
     * @refactor yamini.s
     */

    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-RM-MODULE-1001")
    public void testToVerifyModuleRemoval(Dut device) {
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-RM-MODULE-1001");
	LOGGER.info("TEST DESCRIPTION: Test to verify removal of SoundKernel, mtp-tools, libsven");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify soundkernel module removed status");
	LOGGER.info("2. Verify libsven_modules removed status");
	LOGGER.info("3. Verify mtp-tools removed status");
	LOGGER.info("4. Verify ntpd process running status in Arm side");
	LOGGER.info("5. Verify ntpd client running status in Atom side");
	LOGGER.info("6. Verify COSAXcalibur.XML file removed status");
	LOGGER.info("7. Verify files listed in Acceptance criteria 1 of removed status in Atom side ");
	LOGGER.info("8. Verify files listed in Acceptance criteria 2 of removed status in Atom side ");
	LOGGER.info("9. Verify Atom Console files status from Atom side");
	LOGGER.info("10. Verify 13 Arm console files listed removed status from Arm side on all platforms");
	LOGGER.info("#######################################################################################");

	// variable declaration begins
	// Status of test script verification
	boolean status = false;
	// Test case id
	String testCaseId = "TC-RDKB-RM-MODULE-101";
	// Test step number
	String stepNumber = "s1";
	// String to store error message
	String errorMessage = null;
	// Variable to store atom sync available
	boolean isAtomSyncAvailable = false;
	// String to store device model
	String deviceModel = null;
	// String to store response
	String response = null;
	// variable declaration ends

	try {

	    stepNumber = "s1";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION: Verify soundkernel module removed status");
	    LOGGER.info("STEP 1: ACTION: Execute command: find / -iname sound");
	    LOGGER.info("STEP 1: EXPECTED: Should not present the soundKernal module in device");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "SoundKernal module present in device";
	    // verify whether the atom sync is available in given device
	    isAtomSyncAvailable = CommonMethods.isAtomSyncAvailable(device, tapEnv);
	    status = isAtomSyncAvailable
		    ? (!BroadBandCommonUtils.isFilePresentOnDeviceAtom(tapEnv, device,
			    BroadBandTestConstants.MODULE_SOUND_KERNEL))
		    : (!BroadBandCommonUtils.isFilePresentOnDevice(tapEnv, device,
			    BroadBandTestConstants.MODULE_SOUND_KERNEL));
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL: Successfully verified soundKernal module not present in device");
	    } else {
		LOGGER.error("STEP 1: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    stepNumber = "s2";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION: Verify libsven_modules removed status");
	    LOGGER.info("STEP 2: ACTION: Execute command: find / -iname libsven*");
	    LOGGER.info("STEP 2: EXPECTED: Should not present the libsven_modules in device");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "libsven_modules present in device";
	    status = isAtomSyncAvailable
		    ? (!BroadBandCommonUtils.isFilePresentOnDeviceAtom(tapEnv, device,
			    BroadBandTestConstants.MODULE_LIBSVEN))
		    : (!BroadBandCommonUtils.isFilePresentOnDevice(tapEnv, device,
			    BroadBandTestConstants.MODULE_LIBSVEN));
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL: Successfully verified libsven_modules module not present in device");
	    } else {
		LOGGER.error("STEP 2: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    stepNumber = "s3";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION: Verify mtp-tools removed status");
	    LOGGER.info("STEP 3: ACTION: Execute command: find / -iname libmtp*");
	    LOGGER.info("STEP 3: EXPECTED: Should not present the mtp-tools module in device");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "mtp-tools module present in device";
	    status = isAtomSyncAvailable
		    ? (!BroadBandCommonUtils.isFilePresentOnDeviceAtom(tapEnv, device,
			    BroadBandTestConstants.MODULE_MTP_TOOL))
		    : (!BroadBandCommonUtils.isFilePresentOnDevice(tapEnv, device,
			    BroadBandTestConstants.MODULE_MTP_TOOL));
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL: Successfully verified mtp-tools module not present in device");
	    } else {
		LOGGER.error("STEP 3: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		stepNumber = "s4";
		status = false;
		LOGGER.info("******************************************************************************");
		LOGGER.info("STEP 4: DESCRIPTION: Verify ntpd process running status in Arm side");
		LOGGER.info("STEP 4: ACTION: Execute command on Arm side: ps | grep -i ntpd");
		LOGGER.info("STEP 4: EXPECTED: ntpd process should run in Arm side");
		LOGGER.info("******************************************************************************");
		errorMessage = "Failed to get the ntpd process running status in Arm side";
		response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_TO_GET_NTPD_PROCESS);
		status = CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(
			response, BroadBandCommandConstants.PROCESS_DETAILS_NTPD);
		if (status) {
		    LOGGER.info("STEP 4: ACTUAL: Successfully verified ntpd process running status in Arm side");
		} else {
		    LOGGER.error("STEP 4: ACTUAL: " + errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
		// ##################################################################################################//

		stepNumber = "s5";
		status = false;
		LOGGER.info("******************************************************************************");
		LOGGER.info("STEP 5: DESCRIPTION: Verify ntpd client running status in Atom side");
		LOGGER.info("STEP 5: ACTION: Execute command on Atom side: ps | grep -i ntpd");
		LOGGER.info("STEP 5: EXPECTED: ntpd process should run as client in Atom side");
		LOGGER.info("******************************************************************************");
		errorMessage = "Failed to get the ntpd client process running status in Atom side";
		response = tapEnv.executeCommandOnAtom(device, BroadBandCommandConstants.CMD_TO_GET_NTPD_PROCESS);
		status = CommonMethods.isNotNull(response)
			&& (CommonUtils.isGivenStringAvailableInCommandOutput(response,
				BroadbandPropertyFileHandler.getPropertyKeyForNTPDClient1())
				|| CommonUtils.isGivenStringAvailableInCommandOutput(response,
					BroadbandPropertyFileHandler.getPropertyKeyForNTPDClient2()));
		if (status) {
		    LOGGER.info("STEP 5: ACTUAL: Successfully verified mtp-tools module not present in device");
		} else {
		    LOGGER.error("STEP 5: ACTUAL: " + errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
		// ##################################################################################################//

	    } else {
		LOGGER.info("Step 4 & 5 is applicable for atom sync platforms alone");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, "s4", ExecutionStatus.NOT_APPLICABLE,
			"step is applicable for atom sync platforms alone", false);
		tapEnv.updateExecutionForAllStatus(device, testCaseId, "s5", ExecutionStatus.NOT_APPLICABLE,
			"step is applicable for atom sync platforms alone", false);
	    }

	    stepNumber = "s6";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION: Verify COSAXcalibur.XML file removed status");
	    LOGGER.info("STEP 6: ACTION: Execute command: find / -iname COSAXcalibur.XML");
	    LOGGER.info("STEP 6: EXPECTED: COSAXcalibur.XML file should not be present on the device");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "COSAXcalibur.XML file is present in device";
	    status = !(BroadBandCommonUtils.isFilePresentOnDevice(tapEnv, device,
		    BroadBandTestConstants.FILE_COSAXCALIBUR_XML));
	    if (status) {
		LOGGER.info("STEP 6: ACTUAL: Successfully verified COSAXcalibur.XML file is not present on the device");
	    } else {
		LOGGER.error("STEP 6: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    stepNumber = "s7";

	    if (BroadbandPropertyFileHandler.getStatusForPartialDeviceCheck(device)) {
		String failedFiles = null;
		List<String> failures = new ArrayList<String>();
		status = false;
		LOGGER.info("******************************************************************************");
		LOGGER.info(
			"STEP 7: Verify files listed in Acceptance criteria 1 of removed status in Atom side of atom sync platforms");
		LOGGER.info("STEP 7: ACTION: Execute Command to verify files listed in Acceptance criteria 1");
		LOGGER.info(
			"STEP 7: EXPECTED: Files listed in Acceptance criteria 1  should not be present in Atom side ");
		LOGGER.info("******************************************************************************");
		errorMessage = "Files present in Atom side of the device are ";
		for (BroadBandCommandConstants.QUALCOM_FILES file : BroadBandCommandConstants.QUALCOM_FILES.values()) {
		    status = !BroadBandCommonUtils.isFileExistsonAtom(device, tapEnv, file.getFile());
		    if (!status) {
			failures.add(file.getFile());
		    }
		    LOGGER.info("Presence of file " + file.getFile() + " in Atom side of device: " + status);
		}
		status = failures.size() == 0;
		if (status) {
		    LOGGER.info("STEP 7: ACTUAL: Files listed in Acceptance criteria 1 are not present in Atom side ");
		} else {
		    for (String fi : failures) {
			failedFiles = failedFiles + fi + BroadBandTestConstants.SINGLE_SPACE_CHARACTER;
		    }
		    errorMessage = errorMessage + failedFiles;
		    LOGGER.error("STEP 7: ACTUAL: " + errorMessage + failedFiles);
		}

		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
		// ##################################################################################################//

	    } else {
		tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
			"step is applicable for Atom platform only", false);
	    }

	    stepNumber = "s8";

	    if (BroadbandPropertyFileHandler.getStatusForPartialDeviceCheckX(device)) {
		String failedFiles = BroadBandTestConstants.EMPTY_STRING;
		List<String> failures = new ArrayList<String>();
		status = false;
		LOGGER.info("******************************************************************************");
		LOGGER.info("STEP 8: Verify files listed in Acceptance criteria 2 of  removed status in Atom side ");
		LOGGER.info("STEP 8: ACTION: Execute Command to verify files listed in Acceptance criteria 2 of  removed status in Atom side");
		LOGGER.info(
			"STEP 8: EXPECTED: Files listed in Acceptance criteria 2 should not be present in Atom side ");
		LOGGER.info("******************************************************************************");
		errorMessage = "Files present in Atom side of the device are ";
		for (BroadBandCommandConstants.ATOMSYNC_QUALCOM_FILES file : BroadBandCommandConstants.ATOMSYNC_QUALCOM_FILES
			.values()) {
		    status = !BroadBandCommonUtils.isFileExistsonAtom(device, tapEnv, file.getFile());
		    if (!status) {
			failures.add(file.getFile());
		    }
		}
		status = failures.size() == 0;
		if (status) {
		    LOGGER.info("STEP 8: ACTUAL: Files listed in Acceptance criteria 2 are not present in Atom side");
		} else {
		    for (String fi : failures) {
			failedFiles = failedFiles + fi + BroadBandTestConstants.SINGLE_SPACE_CHARACTER;
		    }
		    errorMessage = errorMessage + failedFiles;
		    LOGGER.error("STEP 8: ACTUAL: " + errorMessage);
		}

		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
		// ##################################################################################################//

	    } else {
		tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
			"step is applicable for Atom platform only", false);
	    }

	    stepNumber = "s9";

	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		String failedFiles = BroadBandTestConstants.EMPTY_STRING;
		;
		List<String> failures = new ArrayList<String>();
		status = false;
		LOGGER.info("******************************************************************************");
		LOGGER.info("STEP 9: Verify 102 Atom Console files listed removed status from Atom side");
		LOGGER.info(
			"STEP 9: ACTION: Execute Command:if [ -f filename ]; then echo \"true\" ; else echo \"false\" ; fi");
		LOGGER.info("STEP 9: EXPECTED: 102 Atom Console files listed must be removed from Atom side");
		LOGGER.info("******************************************************************************");
		errorMessage = "Files present in Atom side of the device are ";
		for (BroadBandCommandConstants.ATOM_REMOVAL_FILES file : BroadBandCommandConstants.ATOM_REMOVAL_FILES
			.values()) {
		    status = !BroadBandCommonUtils.isFileExistsonAtom(device, tapEnv, file.getFile());
		    if (!status) {
			failures.add(file.getFile());
		    }
		}
		status = failures.size() == 0;
		if (status) {
		    LOGGER.info("STEP 9: ACTUAL: 102 Atom Console files listed are not present in Atom side of device");
		} else {
		    for (String fi : failures) {
			failedFiles = failedFiles + fi + BroadBandTestConstants.SINGLE_SPACE_CHARACTER;
		    }
		    errorMessage = errorMessage + failedFiles;
		    LOGGER.error("STEP 9: ACTUAL: " + errorMessage);
		}

		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
		// ##################################################################################################//

	    } else {
		tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
			"step is applicable for Atom platform only", false);
	    }

	    String failedFiles = BroadBandTestConstants.EMPTY_STRING;
	    List<String> failures = new ArrayList<String>();
	    stepNumber = "s10";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP 10: Verify 13 Arm console files removed status from Arm side on all platforms");
	    LOGGER.info("STEP 10: ACTION: Execute Command to verify 13 Arm console files listed must be removed from Arm side side of the device");
	    LOGGER.info(
		    "STEP 10: EXPECTED: 13 Arm console files listed must be removed from Arm side side of the device");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Files present in Arm side of the device are ";
	    for (BroadBandCommandConstants.ARM_REMOVAL_FILES file : BroadBandCommandConstants.ARM_REMOVAL_FILES
		    .values()) {
		status = !CommonUtils.isFileExists(device, tapEnv, file.getFile());
		if (!status) {
		    failures.add(file.getFile());
		}
	    }
	    status = failures.size() == 0;
	    if (status) {
		LOGGER.info(
			"STEP 10: ACTUAL: 13 Arm console files listed are removed from Arm side side of the device");
	    } else {
		for (String fi : failures) {
		    failedFiles = failedFiles + fi + BroadBandTestConstants.SINGLE_SPACE_CHARACTER;
		}
		errorMessage = errorMessage + failedFiles;
		LOGGER.error("STEP 10: ACTUAL: " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error(
		    "Exception Occurred while Verifying removal of SoundKernel, mtp-tools, libsven module and COSAXcalibur.XML"
			    + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
		    false);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-RM-MODULE-1001");
	// ###############################################################//
    }
}
