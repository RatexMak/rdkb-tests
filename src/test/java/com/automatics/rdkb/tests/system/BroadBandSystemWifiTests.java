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
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandSystemWifiTests extends AutomaticsTestBase {

    /**
     * Verify Xconf default value,Hardware make/model,Linux kernel upgrade version
     * <ol>
     * <li>Verify WEBPA command to get the XCONF client firmware download default value.</li>
     * <li>Verify Hardware Make/Model of the device in Xconf logs</li>
     * <li>Verify Linux Kernel Upgrade Version.</li>
     * </ol>
     * 
     * @param settop
     *            {@link Dut}
     * @author Joseph M
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
    @TestDetails(testUID = "TC-RDKB-SYSTEM-5007",  testDecription = "Verify Xconf default value,Hardware make/model,Linux kernel upgrade version")
    public void testToVerifyHardwareAndLinuxVersion(Dut device) {
	// Variable Declaration begins
	String testCaseId = "";
	String stepNum = "";
	String errorMessage = "";
	boolean status = false;
	String response = null;
	testCaseId = "TC-RDKB-SYSTEM-507";

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-SYSTEM-5007");
	LOGGER.info("TEST DESCRIPTION: Verify Xconf default value,Hardware make/model,Linux kernel upgrade version");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify WEBPA command to get the XCONF client firmware download  default value.");
	LOGGER.info("2. Verify Hardware  Make/Model of the device in xconf logs");
	LOGGER.info("3. Verify Linux Kernel Upgrade Version.");
	LOGGER.info("#######################################################################################");

	LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	LOGGER.info("PRE-CONDITION STEPS");
	LOGGER.info("#######################################################################################");
	LOGGER.info("PRE-CONDITION : DESCRIPTION : Reboot the device.");
	LOGGER.info("PRE-CONDITION : ACTION :Reboot the device using command: /sbin/reboot.");
	LOGGER.info("PRE-CONDITION : EXPECTED :  Device should be rebooted.");
	LOGGER.info("#######################################################################################");
	status = CommonUtils.rebootAndWaitForIpAcquisition(tapEnv, device);
	errorMessage = "Unable to reboot the device.";
	if (status) {
	    LOGGER.info("PRE-CONDITION : ACTUAL : Pre condition executed successfully");
	} else {
	    LOGGER.error("PRE-CONDITION : ACTUAL : Pre condition failed");
	    throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION :FAILED : "
		    + errorMessage);
	}
	LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
	try {
	    stepNum = "S1";
	    errorMessage = "The XCONF client firmware download check default value is not as false.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify WEBPA command to get the XCONF client firmware download  default value");
	    LOGGER.info("STEP 1: ACTION : Execute the Below Command:curl -X GET -H 'Authorization: Bearer <SAT TOKEN>' -H 'content-type:application/json' -H 'X-Webpa-Atomic:true' -k -i https://<url:port>/api/v2/device/mac:<MAC Address>/config?names=Device.X_COMCAST-COM_Xcalibur.Client.xconfCheckNow");
	    LOGGER.info("STEP 1: EXPECTED : The output value false confirms the XCONF client firmware download check default value");
	    LOGGER.info("**********************************************************************************");
	    String xconfDefaultValue = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_TRIGGERING_XCONF_CDL);
	    LOGGER.info("Xconf Default value  is = " + xconfDefaultValue);
	    if (CommonMethods.isNotNull(xconfDefaultValue)
		    && xconfDefaultValue.equalsIgnoreCase(BroadBandTestConstants.FALSE)) {
		status = true;
	    }
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : XCONF client firmware download check default value retrieved is false");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S2";
	    errorMessage = "Unable to get the Hardware  Make/Model of the device in logs.";
	    status = false;
	    boolean modelInXconfStatus = false;
	    String command = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Verify Hardware  Make/Model of the device in xconf logs.");
	    LOGGER.info("STEP 2: ACTION : Get the Hardware Make/Model by using below command 'grep -i \"XCONF SCRIPT : MODEL\" /rdklogs/logs/ArmConsolelog.txt.0' for atom devices and 'grep -i \"XCONF SCRIPT\" /rdklogs/logs/Consolelog.txt.0' for arm devices and 'grep -i \"XCONF SCRIPT\" /rdklogs/logs/xconf.txt.0' for other devices");
	    LOGGER.info("STEP 2: EXPECTED : Hardware  Make/Model of the device should be displayed .");
	    LOGGER.info("**********************************************************************************");
	    long startTime = System.currentTimeMillis();
	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		command = BroadBandCommandConstants.XCONF_CURRENT_MODEL_ARM_CONSOLE_LOGS;
	    } 
	else{
		command = BroadBandCommandConstants.XCONF_CURRENT_MODEL;
	    }
	    do {
		response = tapEnv.executeCommandUsingSsh(device, command);
		if (CommonMethods.isNotNull(response)) {
			modelInXconfStatus = CommonUtils.patternSearchFromTargetString(response, device.getModel());
			LOGGER.info("Model Name in log status is" + modelInXconfStatus);
		}
		status = modelInXconfStatus;
	    } while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS
		    && !status
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Able to get the Hardware version and Model of the device in xconf logs ");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S3";
	    errorMessage = "Linux kernel version is not displayed in Arm Console";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Verify Linux Kernel Upgrade Version in ArmConsole");
	    LOGGER.info("STEP 3: ACTION : Get the Linux Kernel Upgrade Version by using the command 'uname -r'");
	    LOGGER.info("STEP 3: EXPECTED : Linux Kernel Version  (LTS version) should be displayed in ArmConsole");
	    LOGGER.info("**********************************************************************************");
	    response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_LINUX_VERSION);
	    LOGGER.info("Response for linux kernel upgrade is" + response);
	    status = CommonUtils.patternSearchFromTargetString(response.trim(),
		    tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_LINUX_KERNEL_VERSION).trim());
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : Linux Kernel Version is verified successfully.");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils
		    .updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage, true);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-SYSTEM-5007");
    }
}
