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
import com.automatics.device.Device;
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DataMountUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class DataMountExecutableTest extends AutomaticsTestBase {
    /**
     *
     * Test Case # 1: Verify the data only partition /nvram is mounted as non-executable.
     *
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>S1) Verify executing the shell script on data only mount partition /nvram.</li>
     * <li>S2) Verify executing the shell script on data only mount partition /nvram, after setting executable
     * permission for all users.</li>
     * </ol>
     *
     * @author BALAJI V
     * @refactor Govardhan
     * @param device
     *            {@link Dut}
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
	    TestGroup.NEW_FEATURE, TestGroup.SYSTEM })

    @TestDetails(testUID = "TC-RDKB-SYSTEM-5502")
    public void testNvramMount(Dut device) {
	String testCaseId = "TC-RDKB-SYSTEM-552";
	boolean result = false;
	String errorMessage = null;
	String step = "s1";
	try {
	    result = CommonUtils.doesDirectoryExist(device, tapEnv, BroadBandCommandConstants.MOUNT_NVRAM);
	    LOGGER.info("### PRE-CONDITION ### DATA ONLY MOUNT " + BroadBandCommandConstants.MOUNT_NVRAM + " EXISTS : "
		    + result);
	    if (!result) {
		errorMessage = "DATA ONLY MOUNT " + BroadBandCommandConstants.MOUNT_NVRAM
			+ " DOES NOT EXIST. HENCE BLOCKING THE EXECUTION.";
		LOGGER.error(errorMessage);
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
	    }

	    String firmwareVersion = ((Device) device).getFirmwareVersion();
	    LOGGER.info("DEVICE FIRMWARE VERSION: " + firmwareVersion);
	    boolean isProdBuild = CommonMethods.isNotNull(firmwareVersion)
		    && BroadbandPropertyFileHandler.verifyIsProdBuildOnDevice(firmwareVersion);
	    LOGGER.info("IS PROD BUILD: " + isProdBuild);

	    /**
	     * S1) Verify executing the shell script on data only mount partition /nvram.
	     */
	    LOGGER.info("STEP 1: DESCRIPTION : Verify executing the shell script on data only mount partition /nvram");
	    LOGGER.info(
		    "STEP 1: ACTION : Create a shell script using command: echo -e \"#!/bin/bash\\n echo \"\"Hello World\"\" > /nvram/rdkb10618.sh and Execute the script as: /nvram/./rdkb10618.sh");
	    LOGGER.info(
		    "STEP 1: EXPECTED: Shell Script must be created successfully and when executed It must throw 'Permission denied'");
	    result = DataMountUtils.createShellScript(tapEnv, device, BroadBandCommandConstants.MOUNT_NVRAM);
	    LOGGER.info("SHELL SCRIPT CREATED FOR EXECUTION: " + result);
	    if (!result) {
		errorMessage = "TEST SHELL SCRIPT COULD NOT BE CREATED. HENCE BLOCKING THE EXECUTION.";
		LOGGER.error(errorMessage);
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
	    }
	    result = DataMountUtils.verifyScriptExecution(tapEnv, device, BroadBandCommandConstants.MOUNT_NVRAM,
		    isProdBuild);
	    errorMessage = isProdBuild
		    ? "Shell Script got Created successfully and did not thrown Permission denied error on Prod build."
		    : "Shell Script got Created successfully and did not thrown Permission denied error.";
	    String successMessage = isProdBuild
		    ? "Shell Script got Created successfully and thrown Permission denied error on Prod build."
		    : "Shell Script got Created successfully and thrown Permission denied error.";
	    if (result) {
		LOGGER.info("STEP 1: ACTUAL : " + successMessage);
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * S2) Verify executing the shell on data only mount partition /nvram, after setting executable permission
	     * for all users.
	     */
	    step = "s2";
	    result = false;
	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Verify executing shell script with permission as '777' on /nvram mount.");
	    LOGGER.info(
		    "STEP 2: ACTION : Execute command: chmod 777 /nvram/rdkb10618.sh and Execute the script as: /nvram/./rdkb10618.sh");
	    LOGGER.info(
		    "STEP 2: EXPECTED: Shell Script must be executed successfully, In case of prod build it should throw Permission denied.");
	    result = DataMountUtils.verifyScriptExecutionAfterGrantingPermission(tapEnv, device,
		    BroadBandCommandConstants.MOUNT_NVRAM, isProdBuild);

	    errorMessage = isProdBuild
		    ? "Shell Script got executed successfully and did not thrown Permission denied error on Prod build."
		    : "Shell Script did not get executed successfully and thrown Permission denied error.";
	    successMessage = isProdBuild
		    ? "Shell Script did not get executed successfully and thrown Permission denied error on Prod build."
		    : "Shell Script got executed successfully and did not thrown Permission denied error.";
	    if (result) {
		LOGGER.info("STEP 2: ACTUAL : " + successMessage);
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error(
		    "EXCEPTION OCCURRED WHILE VALIDATING THE DATA ONLY STORAGE PARTITION /nvram IS MOUNTED AS NON-EXECUTABLE: "
			    + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, result, errorMessage, true);
	} finally {
	    LOGGER.info("### POST-CONDITION ### REMOVE THE TEST SHELL SCRIPT: " + BroadBandCommandConstants.MOUNT_NVRAM
		    + BroadBandCommandConstants.FILE_TEST_SHELL_SCRIPT);
	    result = CommonUtils.deleteFile(device, tapEnv,
		    BroadBandCommandConstants.MOUNT_NVRAM + BroadBandCommandConstants.FILE_TEST_SHELL_SCRIPT);
	    LOGGER.info("### POST-CONDITION ### REMOVED THE TEST SHELL SCRIPT: " + result);
	}
    }

    /**
     *
     * Test Case # 2: Verify the data only partition /nvram2 is mounted as non-executable.
     *
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>S1) Verify executing the shell script on data only mount partition /nvram2.</li>
     * <li>S2) Verify executing the shell script on data only mount partition /nvram2, after setting executable
     * permission for all users.</li>
     * </ol>
     *
     * @author BALAJI V
     * @refactor Govardhan
     * 
     * @param device
     *            {@link Dut}
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
	    TestGroup.NEW_FEATURE, TestGroup.SYSTEM })

    @TestDetails(testUID = "TC-RDKB-SYSTEM-5503")
    public void testNvram2Mount(Dut device) {
	String testCaseId = "TC-RDKB-SYSTEM-553";
	boolean result = false;
	String errorMessage = null;
	String step = "s1";
	try {
	    result = CommonUtils.doesDirectoryExist(device, tapEnv, BroadBandCommandConstants.MOUNT_NVRAM2);
	    LOGGER.info("### PRE-CONDITION ### DATA ONLY MOUNT " + BroadBandCommandConstants.MOUNT_NVRAM2 + " EXISTS : "
		    + result);
	    if (!result) {
		errorMessage = "DATA ONLY MOUNT " + BroadBandCommandConstants.MOUNT_NVRAM2
			+ " DOES NOT EXIST. HENCE BLOCKING THE EXECUTION.";
		LOGGER.error(errorMessage);
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
	    }

	    String firmwareVersion = ((Device) device).getFirmwareVersion();
	    LOGGER.info("DEVICE FIRMWARE VERSION: " + firmwareVersion);
	    boolean isProdBuild = CommonMethods.isNotNull(firmwareVersion)
		    && BroadbandPropertyFileHandler.verifyIsProdBuildOnDevice(firmwareVersion);
	    LOGGER.info("IS PROD BUILD: " + isProdBuild);

	    /**
	     * S1) Verify executing the shell script on data only mount partition /nvram2.
	     */
	    LOGGER.info(
		    "STEP 1: DESCRIPTION : Verify executing the shell script on data only mount partition /nvram2.");
	    LOGGER.info(
		    "STEP 1: ACTION : Create a shell script using command: echo -e \"#!/bin/bash\\n echo \"Hello World\"\" > /nvram2/rdkb10618.sh and Execute the script as: /nvram2/./rdkb10618.sh");
	    LOGGER.info(
		    "STEP 1: EXPECTED: Shell Script must be created successfully and when executed it must throw 'Permission denied'");
	    result = DataMountUtils.createShellScript(tapEnv, device, BroadBandCommandConstants.MOUNT_NVRAM2);
	    if (!result) {
		errorMessage = "TEST SHELL SCRIPT COULD NOT BE CREATED. HENCE BLOCKING THE EXECUTION.";
		LOGGER.error(errorMessage);
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
	    }
	    result = DataMountUtils.verifyScriptExecution(tapEnv, device, BroadBandCommandConstants.MOUNT_NVRAM2,
		    isProdBuild);
	    errorMessage = isProdBuild
		    ? "Shell Script got Created successfully and did not thrown Permission denied error on Prod build."
		    : "Shell Script got Created successfully and did not thrown Permission denied error.";
	    String successMessage = isProdBuild
		    ? "Shell Script got Created successfully and thrown Permission denied error on Prod build."
		    : "Shell Script got Created successfully and thrown Permission denied error.";
	    if (result) {
		LOGGER.info("STEP 1: ACTUAL : " + successMessage);
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * S2) Verify executing the shell script on data only mount partition /nvram2, after setting executable
	     * permission for all users.
	     */
	    step = "s2";
	    result = false;
	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Verify executing shell scrip with permission as '777' on /nvram2 mount.");
	    LOGGER.info("STEP 2: ACTION : Execute command: chmod 777 /nvram2/rdkb10618.sh and Execute the script as: nvram2/./rdkb10618.sh");
	    LOGGER.info(
		    "STEP 2: EXPECTED : Shell Script must be executed successfully, In case of prod build it should throw Permission denied.");
	    result = DataMountUtils.verifyScriptExecutionAfterGrantingPermission(tapEnv, device,
		    BroadBandCommandConstants.MOUNT_NVRAM2, isProdBuild);
	    errorMessage = isProdBuild
		    ? "Shell Script got executed successfully and did not thrown Permission denied error on Prod build."
		    : "Shell Script did not get executed successfully and thrown Permission denied error.";
	    successMessage = isProdBuild
		    ? "Shell Script did not get executed successfully and thrown Permission denied error on Prod build."
		    : "Shell Script got executed successfully and did not thrown Permission denied error.";
	    if (result) {
		LOGGER.info("STEP 2: ACTUAL : " + successMessage);
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error(
		    "EXCEPTION OCCURRED WHILE VALIDATING THE DATA ONLY STORAGE /nvram2 PARTITION IS MOUNTED AS NON-EXECUTABLE: "
			    + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, result, errorMessage, true);
	} finally {
	    LOGGER.info("### POST-CONDITION ### REMOVE THE TEST SHELL SCRIPT: " + BroadBandCommandConstants.MOUNT_NVRAM2
		    + BroadBandCommandConstants.FILE_TEST_SHELL_SCRIPT);
	    result = CommonUtils.deleteFile(device, tapEnv,
		    BroadBandCommandConstants.MOUNT_NVRAM2 + BroadBandCommandConstants.FILE_TEST_SHELL_SCRIPT);
	    LOGGER.info("### POST-CONDITION ### REMOVED THE TEST SHELL SCRIPT: " + result);
	}
    }

}
