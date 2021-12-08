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
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.WebPaParamConstants;
import com.automatics.rdkb.utils.BroadBandRfcFeatureControlUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.AutomaticsPropertyUtility;

/**
 * Class for Enable Disable Feature Via DCM
 */
public class BroadBandDCMTest extends AutomaticsTestBase{
    /**
     * Verify configurable RFC check-in trigger using tr181 parameter
     * <ol>
     * <li>Clear dcmrfc.log and trigger Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Control.RetrieveNow</li>
     * <li>Verify tr181 parameter has been enabled by RFC script executed after trigger</li>
     * </ol>
     * 
     * @param device
     *            {@link Dut}
     * 
     * @author Ashwin sankara
     * @refactor Govardhan
     * 
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-RFC-1001")
    public void testVerifyConfigurableRFC(Dut device) {

	// Variable Declaration begins
	String testCaseId = "";
	String stepNum = "";
	String errorMessage = "";
	boolean status = false;
	BroadBandResultObject result = new BroadBandResultObject();
	// Variable Declation Ends

	testCaseId = "TC-RDKB-RFC-101";

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-RFC-1001");
	LOGGER.info("TEST DESCRIPTION: Verify configurable RFC check-in trigger using tr181 parameter");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Clear dcmrfc.log and trigger Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Control.RetrieveNow");
	LOGGER.info("2. Verify tr181 parameter has been enabled by RFC script executed after trigger");

	LOGGER.info("#######################################################################################");

	try {

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    LOGGER.info(
		    "PRE-CONDITION : DESCRIPTION : Configure RFC payload data to enable tr181 parameter when RFC retrieve now is triggered.");
	    LOGGER.info(
		    "PRE-CONDITION : ACTION : Copy and update RFC_CONFIG_SERVER_URL in /nvram/rfc.properties and post the payload");
	    LOGGER.info("PRE-CONDITION : EXPECTED : Successfully copied file and posted payload data");

	    if (BroadBandRfcFeatureControlUtils.executePreconditionForRfcTests(device, tapEnv,
		    AutomaticsPropertyUtility.getProperty(BroadBandTestConstants.PROP_KEY_TO_ENABLE_DISABLE_CLOUD_UPLOAD_ENCRYPTION)
			    .replace(BroadBandTestConstants.STRING_TO_CHANGE_ENCRYPT_CLOUD_UPLOAD_STATUS,
				    BroadBandTestConstants.TRUE))) {
		LOGGER.info("PRE-CONDITION : ACTUAL : Pre condition executed successfully");
	    }

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s1";
	    errorMessage = "Unable to set value of Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Control.RetrieveNow to unsigned integer 1";
	    status = false;

	    LOGGER.info(
		    "STEP 1: DESCRIPTION : Clear dcmrfc.log and trigger Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Control.RetrieveNow");
	    LOGGER.info(
		    "STEP 1: ACTION : Clear dcmrfc.log and execute webpa command to set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Control.RetrieveNow");
	    LOGGER.info("STEP 1: EXPECTED : Successfully set RFC.RetrieveNow parameter with unsigned int 1");

	    status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    WebPaParamConstants.WEBPA_PARAM_IMMEDIATE_RFC_CHECK_IN, BroadBandTestConstants.CONSTANT_2,
		    BroadBandTestConstants.STRING_VALUE_ONE);

	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : Successfully set RFC.RetrieveNow parameter with unsigned int 1");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s2";
	    errorMessage = "Failed to set parameter value using TR181 trigger of RFC";
	    status = false;

	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Verify tr181 parameter has been enabled by RFC script executed after trigger");
	    LOGGER.info(
		    "STEP 2: ACTION : Verify parameter value in rfc-parsed.txt, rfc_configdata.txt, dcmrfc.log and tr181 get.");
	    LOGGER.info("STEP 2: EXPECTED : Value of  EncryptUpload Enable parameter value is true");

	    result = BroadBandRfcFeatureControlUtils.verifyParameterUpdatedByRfc(device, tapEnv,
		    WebPaParamConstants.WEBPA_PARAM_TO_ENABLE_DISABLE_CLOUD_UPLOAD_ENCRYPTION,
		    BroadBandTestConstants.TRUE);
	    status = result.isStatus();
	    errorMessage = result.getErrorMessage();

	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Value of  EncryptUpload Enable parameter value is true");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    true);
	} finally {

	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    LOGGER.info(
		    "POST-CONDITION : DESCRIPTION : Remove nvram override of rfc.properties and disable tr181 parameter.");
	    LOGGER.info("POST-CONDITION : ACTION : Remove /nvram/rfc.properties and disable tr181 parameter.");
	    LOGGER.info("POST-CONDITION : EXPECTED : WebPA set is successful and parmeter value is false");

	    status = CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device,
		    BroadBandRfcFeatureControlUtils.NVRAM_RFC_PROPERTIES)
		    && BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			    WebPaParamConstants.WEBPA_PARAM_TO_ENABLE_DISABLE_CLOUD_UPLOAD_ENCRYPTION,
			    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE);

	    if (status) {
		LOGGER.info("POST-CONDITION : ACTUAL : Post condition executed successfully");
	    } else {
		LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed");
	    }
	    LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-RFC-1001");
	LOGGER.info("#######################################################################################");
    }

}